(function($) { 

// Models

window.MessageModel = Backbone.Model.extend({

    validate: function(attrs) {
    
        if ("message" in attrs && attrs.message.length > 160) {
            return "Message too long";
        }
    }

});

// Collections

window.MessageCollection = Backbone.Collection.extend({

    model: MessageModel,
    url: '/api/msgs'

});
window.Messages = new MessageCollection();

// Views

window.MessageView = Backbone.View.extend({

    tagName: "div",
    className: "msg",
    template: Handlebars.templates['message.html'],

    events: {
        "click #delete-message" : "deleteMessage",
        "dblclick .msg-input"   : "editMessage",
        "keypress .msg-input"   : "updateMessage"
    },

    initialize: function() {
        this.model.view = this;
    }, 

    render: function() {
        $(this.el).html(this.template(this.model.toJSON()));
        return this;
    },

    editMessage: function() {
        this.$(".msg-input").removeAttr("readonly");
    }, 

    updateMessage: function(e) {
        if (e.keyCode != 13) return;
        this.$(".msg-input").attr("readonly","true");
        this.model.save({ message: this.$(".msg-input").val() }, {
            success: function() {
                $("#alert-msg").html("Message updated successfully").attr('class','alert-msg-ok');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            },
            error: function() {
                $("#alert-msg").html("An error ocurred while updating this message").attr('class','alert-msg-error');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            }  
        });
    },

    deleteMessage: function() {
        this.model.destroy({
            success: function() {
                $("#alert-msg").html("Message deleted successfully").attr('class','alert-msg-ok');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            }, 
            error: function() {
                $("#alert-msg").html("An error ocurred while deleting this message").attr('class','alert-msg-error');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            }
        });
        $(this.el).slideUp();
    } 

});

window.MessagesView = Backbone.View.extend({

    el: $("#wall-content"),

    events: {
        "keypress #new-msg-input"  : "createMessage"
    },

    initialize: function() {
        this.input = this.$("#new-msg-input");
        Messages.bind('add',   this.addMessage, this);
        Messages.bind('reset', this.addAll, this);
        Messages.fetch();
    },

    addMessage: function(msg) {
        var view = new MessageView({model: msg});
        this.$("#msgs").prepend(view.render().el);
        $(view.el).hide().slideDown();
    },

    addAll: function() {
        Messages.each(this.addMessage);
    },

    createMessage: function(e) {
        if (e.keyCode != 13) return;
        Messages.create({ message: this.input.val() }, {
            success: function() {
                $("#alert-msg").html("Message created successfully").attr('class','alert-msg-ok');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            }, 
            error: function() {
                $("#alert-msg").html("An error ocurred while creating this message").attr('class','alert-msg-error');
                $("#alert-msg").stop().fadeIn("slow").delay(2000).slideUp();
            }
        }); 
        this.input.val("");
    }

});
window.Wall = new MessagesView();


})(jQuery)
