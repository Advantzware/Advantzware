
#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Collections;
using System.Configuration;
using System.Web;
using System.Threading;
using System.Globalization;
using System.Text;
#endregion

public partial class view_apply_cshrcpt : System.Web.UI.Page
{
    string oldinv = "";
    string recid = "";
   protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "view_apply_cshrcpt.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            lblUser.Text = UserLogin.UserName;
            Session["Customers_Company"] = labelcompany.Text;

            if (aUsers == "external")
            {


            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }

        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                FormView1.DataBind();   
                if (Session["view_cash_receipt_memo_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_cash_receipt_memo_reckey_index"]);
                        Session["view_apply_item_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_apply_item_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }            
            

        } //  ! Page.IsPostBack

             


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       if (FormView1.CurrentMode == FormViewMode.Insert)
       {           
       }
       if (FormView1.CurrentMode == FormViewMode.Edit)
       {           
       }
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {           
           
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["view_apply_memo_reckey_rec"] = reckey.Text.Trim();
               
           }
           catch { }
           FormView2.Visible = true;
           GridView1.Visible = true;
           
       }
   }  
   


   

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }    

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }


    protected void lnk_viewcustomers_Click(object sender, EventArgs e)
    {
        Response.Redirect("view_apply_cshrcpt.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("apply_cshmmo_list.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_cash_receipt_memo_reckey_index"] = GridView1.SelectedIndex;
        Session["view_apply_item_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

    protected void UpdateButton_Formview2_Click(object sender, EventArgs e)
    {
        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox disc = (TextBox)FormView2.FindControl("disTextBox");        
        TextBox app = (TextBox)FormView2.FindControl("ttlappTextBox");
        
        

        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        if (baldue.Text == "")
            baldue.Text = "0.00";
        if (app.Text == "")
            app.Text = "0.00";
        if (disc.Text == "")
            disc.Text = "0.00";
        UserClass UserLogin = (UserClass)Session["User"];
        
        account val = new account();

        bool check = val.validApplyReApply("ValidateUpdate", "", UserLogin.UserName, "", Convert.ToInt32(invno.Text.Trim()), invdate.Text.Trim(), Convert.ToDecimal(baldue.Text.Trim()), Convert.ToDecimal(app.Text.Trim()), Convert.ToDecimal(disc.Text.Trim()), Convert.ToString(Session["cash_receipt_memo_reckey_rec"]), Convert.ToString(Session["view_cash_receipt_memo_reckey"]));

        string value = Convert.ToString(check);
        if (value == "True")
        {
                ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Update";
                //ObjectDataSource3.SelectParameters["prmComp"].DefaultValue =  ; 
                ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
                ObjectDataSource3.SelectParameters["prminv"].DefaultValue = invno.Text.Trim(); ;
                ObjectDataSource3.SelectParameters["prminvdt"].DefaultValue = invdate.Text.Trim();
                ObjectDataSource3.SelectParameters["prmbal"].DefaultValue = baldue.Text.Trim();
                ObjectDataSource3.SelectParameters["prmdisc"].DefaultValue = disc.Text.Trim();
                ObjectDataSource3.SelectParameters["prmapp"].DefaultValue = app.Text.Trim();                                
                ObjectDataSource3.SelectParameters["prmReckey"].DefaultValue = reckey.Text.Trim();

                FormView2.ChangeMode(FormViewMode.ReadOnly);
                Response.Write("<script>window.location.href='view_apply_cshrcpt.aspx'</script>");
            }
    }
    
    

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        
        if (FormView2.CurrentMode == FormViewMode.Edit)
        {
            TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
            invno.Focus();
            GridView1.Visible = false;
        }
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyTextBox");
                Session["view_cash_receipt_memo_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            
            //try
            //{
            //    if (FormView2.DataItemCount.ToString() == "0")
            //        AddNewFormView2Button.Visible = true;
            //    else
            //        AddNewFormView2Button.Visible = false;
            //}
            //catch { }
        }
    }
    protected void CancelButton_FormView2_Delete(object sender, EventArgs e)
    {
        TextBox reckey = (TextBox)FormView2.FindControl("reckeyTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];
        //try
        //{
        //    account con = new account();
        //    DataSet ds = new DataSet();            
        //    ds = con.EnterEditCashReceiptView("AddNew", "", UserLogin.UserName, "", 0, "", 0, 0, 0, 0, "", "", 0, "", reckey.Text.Trim());
        //}
        //catch { }
        FormView1.Visible = true;
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='view_apply_cshrcpt.aspx'</script>");
    }
    

    

    protected void img_btn_exit_click(object sender, EventArgs e)
    {
        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }

   
    protected void Invoice_OnTextChange(object sender, EventArgs e)
    {
        TextBox invno = (TextBox)FormView2.FindControl("invTextBox");
        TextBox invdate = (TextBox)FormView2.FindControl("invdateLabel");
        TextBox baldue = (TextBox)FormView2.FindControl("baldueLabel");
        TextBox disc = (TextBox)FormView2.FindControl("disTextBox");
        TextBox app = (TextBox)FormView2.FindControl("ttlappTextBox");

        Label cust = (Label)FormView1.FindControl("custnoLabel");

        UserClass UserLogin = (UserClass)Session["User"];
        try
        {
            account con = new account();
            DataSet ds = new DataSet();
            ds = con.CrDbInvoiceLookup("Search", UserLogin.UserName, "inv", "EQUAL", invno.Text.Trim(), cust.Text.Trim());


            //invno.Text = Convert.ToString(ds.Tables[0].Rows[0][11]);
            invdate.Text = Convert.ToString(ds.Tables[0].Rows[0][2]);
            baldue.Text = Convert.ToString(ds.Tables[0].Rows[0][5]);


            app.Focus();
        }
        catch { }

    }
    

}
