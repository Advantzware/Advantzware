using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class QuoteNotes: System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        try
        {
            if (Session["list_rfq_rfq_nos"] != null)
            {
                LinkButton b2quote = (LinkButton)Master.FindControl("list_quote");
                b2quote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("listquoteh") ;
                rfqlist.Attributes.Add("style", "display:none");
            }
        }
        catch
        {
        }
    }

    protected void Page_Load(object sender, EventArgs e)
    {
              
        if (Session["list_rfq_rfq_nos"] == null)
        {
            try
            {
                LinkButton bquote = (LinkButton)Master.FindControl("ImageButton1");
                bquote.Visible = false;
                HtmlGenericControl rfqlist = (HtmlGenericControl)this.Page.Master.FindControl("rfqlist");
                rfqlist.Attributes.Add("style", "display:none");
                Hyperlink1.Visible = false;
            }
            catch { }
        }

           
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
            {                
                                             
                string vUserId = UserLogin.UserName;
                string vPage = "ViewQuote.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                //lblComp.Text = PrmComp;
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");

                }
            }
            //Label name = (Label)Master.FindControl("lbl_page");
            //name.Text = "View Job";
        if (!Page.IsPostBack)
        {
           
        }
        /*ImageButton viewquote = (ImageButton)Master.FindControl("machhrs");
        viewquote.ImageUrl = "~/Images/notes 1.jpg";*/
        }

   
    

    protected void PrintButtonClick(object sender, EventArgs e)
    {
        if (!Request.Browser.Browser.Contains("Safari"))
            Response.Write("<script>window.open('PrintQuote.aspx'); target='_blank'</script>");
        else
            Response.Redirect("BrowseQuote.aspx"); 
    }

  
    
  
    
    protected void Formview2_onbatabound(object sender, EventArgs e)
    {
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox note = (TextBox)FormView2.FindControl("qtqty_comment1TextBox");
            note.Focus();
            
        }
        if (FormView2.CurrentMode == FormViewMode.Insert)
        {
           
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            
            
        }
    }

    protected void Formview2_update_button_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        TextBox note1 = (TextBox)FormView2.FindControl("qtqty_comment1TextBox");
        TextBox note2 = (TextBox)FormView2.FindControl("qtqty_comment2TextBox");
        TextBox note3 = (TextBox)FormView2.FindControl("qtqty_comment3TextBox");
        TextBox note4 = (TextBox)FormView2.FindControl("qtqty_comment4TextBox");
        TextBox note5 = (TextBox)FormView2.FindControl("qtqty_comment5TextBox");

        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmNote1"].DefaultValue = note1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNote2"].DefaultValue = note2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNote3"].DefaultValue = note3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNote4"].DefaultValue = note4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNote5"].DefaultValue = note5.Text.Trim();


        FormView2.ChangeMode(FormViewMode.ReadOnly);
            
        

    }

    

    
}

