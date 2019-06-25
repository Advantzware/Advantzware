
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

public partial class view_apinv_inquiry : System.Web.UI.Page
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
            string vPage = "apinv_inquiry.aspx";
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

        if (!Page.IsPostBack)
        {
            if (Session["User"] != null)
            {
                
                
                if (Session["view_inv_vendor_reckey_index"] != null)
                {
                    try
                    {
                        GridView1.SelectedIndex = Convert.ToInt32(Session["view_inv_vendor_reckey_index"]);
                        Session["view_inv_vendor_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }
                else
                {
                    try
                    {
                        GridView1.SelectedIndex = 0;
                        Session["view_inv_vendor_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
                    }
                    catch { }
                }

            }
            

        } //  ! Page.IsPostBack

        StringBuilder str = new StringBuilder();
        str.Append("<script language=javascript>");
        str.Append("function update(e){");

        str.Append("document.forms[0].FormView2_actnumTextBox.value=e[1];");
        str.Append("var line = document.getElementById('FormView2_arlineLabel');");
        str.Append("line.value = e[0];");
        str.Append("document.forms[0].FormView2_inv_qtyTextBox.value=e[2];");
        str.Append("var tmsf2 = document.getElementById('FormView2_totl_msfLabel');");
        str.Append("tmsf2.value = e[3];");
        str.Append("var ino = document.getElementById('FormView2_i_noLabel');");
        str.Append("ino.value = e[4];");
        str.Append("var sn = document.getElementById('FormView2_snumLabel');");
        str.Append("sn.value = e[5];");
        str.Append("var accdesc = document.getElementById('FormView2_actdscrlabel');");
        str.Append("accdesc.value = e[6];}");


        

        str.Append("</script>");

        // register the javascript into the Page
        if (!ClientScript.IsClientScriptBlockRegistered(this.GetType(), "update"))
        {
            Page.RegisterClientScriptBlock("update", str.ToString());
        }
       
       


    }

   protected void FormView1_OnDataBound(object sender, EventArgs e)
   {
     
       
       if (FormView1.CurrentMode == FormViewMode.ReadOnly)
       {
           try
           {
               Button but1 = (Button)FormView1.FindControl("Button1");
               Label stat = (Label)FormView1.FindControl("statsLabel");
               if (stat.Text == "R")
                   but1.OnClientClick = "return confirm('Are you sure you wish to hold this Vendor Invoice?');";
                
               if (stat.Text == "H")
                   but1.OnClientClick = "return confirm('Are you sure you wish to release this Vendor Invoice?');";
           } 
           catch{}
           try
           { 
               
               Label reckey = (Label)FormView1.FindControl("reckeyLabel");
               Session["vendor_invoice_reckey_rec"] = reckey.Text.Trim();
               
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
        Response.Redirect("view_apinv_inquiry.aspx");
    }
    protected void lnk_listinvoice(object sender, EventArgs e)
    {
        Response.Redirect("apinv_inquiry.aspx");
    }

    

    protected void GridView1_SelectedIndex(object sender, EventArgs e)
    {
        Session["view_inv_vendor_reckey_index"] = GridView1.SelectedIndex;
        Session["view_inv_vendor_reckey"] = ((Label)GridView1.SelectedRow.FindControl("reclabel")).Text;
    }

   

    protected void FormView2_OnDataBound(object sender, EventArgs e)
    {
        
        if (FormView2.CurrentMode == FormViewMode.ReadOnly)
        {
            try
            {
                Label reckey = (Label)FormView2.FindControl("ReckeyLabel");
                Session["view_inv_vendor_reckey"] = reckey.Text.Trim();

            }
            catch { }
            GridView1.Visible = true;
            
        }
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

    
    

}
