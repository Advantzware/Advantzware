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

/// <summary>
/// Summary description for Class1
/// </summary>
public partial class Master5 : System.Web.UI.MasterPage
{
    public Master5()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    protected void Page_Load(object sender, EventArgs e)
    {
        string fname = this.Page.GetType().Name.ToString();


        if (fname == "list_rfqs_aspx")
        {

            lilist_rfqs.Attributes.Add("class", "selected");
        }
        else if (fname == "view_rfqs_aspx")
        {
            liview_rfqs.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_list_item_aspx")
        {
            lilist_item.Attributes.Add("class", "selected");
        }
        else if (fname == "rfqitem_aspx")
        {
            lirfq_item.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_size_aspx")
        {
            lirfq_size.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_itemspec_aspx")
        {
            lirfq_itemspec.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_material_aspx")
        {
            lirfq_material.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_printing_aspx")
        {
            lirfq_printing.Attributes.Add("class", "selected");
        }
        else if (fname == "rfq_ship_aspx")
        {
            lirfq_shipping.Attributes.Add("class", "selected");
        }
        else
        {
            liquote.Attributes.Add("class", "selected");
        }
        if (!Page.IsPostBack)
        {

            if (Session["User"] != null)
            {
                UserClass UserLogin = (UserClass)Session["User"];
                lblUser.Text = UserLogin.UserName;


                string vUserId = UserLogin.UserName;
                string vPage = "list_rfqs.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                //Response.Write(Page);
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

                lblComp.Text = PrmComp;
                //Response.Write(vCanRun);
                if (vCanRun == true)
                {
                    lnklist_rfqs.Visible = true;
                   // list_rfqs.Visible = true;

                }

                if (vCanRun == false)
                {
                    lnklist_rfqs.Visible = false;
                    //list_rfqs.Visible = false;

                }


                string vUserIdrfq_item = UserLogin.UserName;
                string vPagerfq_item = "rfqitem.aspx";
                string aUsersrfq_item = null;
                string PrmComprfq_item = null;
                bool vCanCreaterfq_item = false;
                bool vCanRunrfq_item = false;
                bool vCanUpdaterfq_item = false;
                bool vCanDeleterfq_item = false;

                func1 f1rfq_item = new func1();
                //Response.Write(Page);
                f1rfq_item.CheckProgramPermissions(vPagerfq_item, vUserIdrfq_item, ref  vCanCreaterfq_item, ref  vCanRunrfq_item, ref  vCanUpdaterfq_item, ref  vCanDeleterfq_item, ref  PrmComprfq_item, ref  aUsersrfq_item);

                lblComp.Text = PrmComprfq_item;
                //Response.Write(vCanRun);
                if (vCanRunrfq_item == true)
                {
                    lnkrfq_item.Visible = true;
                    //rfq_item.Visible = true;

                }

                if (vCanRunrfq_item == false)
                {
                    lnkrfq_item.Visible = false;
                   // rfq_item.Visible = false;

                }

                string vUserIdrfq_size = UserLogin.UserName;
                string vPagerfq_size = "rfq_size.aspx";
                string aUsersrfq_size = null;
                string PrmComprfq_size = null;
                bool vCanCreaterfq_size = false;
                bool vCanRunrfq_size = false;
                bool vCanUpdaterfq_size = false;
                bool vCanDeleterfq_size = false;

                func1 f1rfq_size = new func1();
                //Response.Write(Page);
                f1rfq_size.CheckProgramPermissions(vPagerfq_size, vUserIdrfq_size, ref  vCanCreaterfq_size, ref  vCanRunrfq_size, ref  vCanUpdaterfq_size, ref  vCanDeleterfq_size, ref  PrmComprfq_size, ref  aUsersrfq_size);

                lblComp.Text = PrmComprfq_size;
                //Response.Write(vCanRun);
                if (vCanRunrfq_size == true)
                {
                    lnkrfq_size.Visible = true;
                   // rfq_size.Visible = true;

                }

                if (vCanRunrfq_size == false)
                {
                    lnkrfq_size.Visible = false;
                    //rfq_size.Visible = false;

                }

                string vUserIdview_rfqs = UserLogin.UserName;
                string vPageview_rfqs = "view_rfqs.aspx";
                string aUsersview_rfqs = null;
                string PrmCompview_rfqs = null;
                bool vCanCreateview_rfqs = false;
                bool vCanRunview_rfqs = false;
                bool vCanUpdateview_rfqs = false;
                bool vCanDeleteview_rfqs = false;

                func1 f1view_rfqs = new func1();
                //Response.Write(Page);
                f1view_rfqs.CheckProgramPermissions(vPageview_rfqs, vUserIdview_rfqs, ref  vCanCreateview_rfqs, ref  vCanRunview_rfqs, ref  vCanUpdateview_rfqs, ref  vCanDeleteview_rfqs, ref  PrmCompview_rfqs, ref  aUsersview_rfqs);

                lblComp.Text = PrmCompview_rfqs;
                //Response.Write(vCanRun);
                if (vCanRunview_rfqs == true)
                {
                    lnkview_rfqs.Visible = true;
                    //view_rfqs.Visible = true;

                }

                if (vCanRunview_rfqs == false)
                {
                    lnkview_rfqs.Visible = false;
                   // view_rfqs.Visible = false;

                }

                string vUserIdrfq_itemspec = UserLogin.UserName;
                string vPagerfq_itemspec = "rfq_itemspec.aspx";
                string aUsersrfq_itemspec = null;
                string PrmComprfq_itemspec = null;
                bool vCanCreaterfq_itemspec = false;
                bool vCanRunrfq_itemspec = false;
                bool vCanUpdaterfq_itemspec = false;
                bool vCanDeleterfq_itemspec = false;

                func1 f1rfq_itemspec = new func1();
                //Response.Write(Page);
                f1rfq_itemspec.CheckProgramPermissions(vPagerfq_itemspec, vUserIdrfq_itemspec, ref  vCanCreaterfq_itemspec, ref  vCanRunrfq_itemspec, ref  vCanUpdaterfq_itemspec, ref  vCanDeleterfq_itemspec, ref  PrmComprfq_itemspec, ref  aUsersrfq_itemspec);

                lblComp.Text = PrmComprfq_itemspec;
                //Response.Write(vCanRun);
                if (vCanRunrfq_itemspec == true)
                {
                    lnkrfq_itemspec.Visible = true;
                    //rfq_itemspec.Visible = true;

                }

                if (vCanRunrfq_itemspec == false)
                {
                    lnkrfq_itemspec.Visible = false;
                    //rfq_itemspec.Visible = false;

                }

                string vUserIdrfq_material = UserLogin.UserName;
                string vPagerfq_material = "rfq_material.aspx";
                string aUsersrfq_material = null;
                string PrmComprfq_material = null;
                bool vCanCreaterfq_material = false;
                bool vCanRunrfq_material = false;
                bool vCanUpdaterfq_material = false;
                bool vCanDeleterfq_material = false;

                func1 f1rfq_material = new func1();
                //Response.Write(Page);
                f1rfq_material.CheckProgramPermissions(vPagerfq_material, vUserIdrfq_material, ref  vCanCreaterfq_material, ref  vCanRunrfq_material, ref  vCanUpdaterfq_material, ref  vCanDeleterfq_material, ref  PrmComprfq_material, ref  aUsersrfq_material);

                lblComp.Text = PrmComprfq_material;
                //Response.Write(vCanRun);
                if (vCanRunrfq_material == true)
                {
                    lnkrfq_material.Visible = true;
                   // rfq_material.Visible = true;

                }

                if (vCanRunrfq_material == false)
                {
                    lnkrfq_material.Visible = false;
                   // rfq_material.Visible = false;

                }
                string vUserIdrfq_printing = UserLogin.UserName;
                string vPagerfq_printing = "rfq_printing.aspx";
                string aUsersrfq_printing = null;
                string PrmComprfq_printing = null;
                bool vCanCreaterfq_printing = false;
                bool vCanRunrfq_printing = false;
                bool vCanUpdaterfq_printing = false;
                bool vCanDeleterfq_printing = false;

                func1 f1rfq_printing = new func1();
                //Response.Write(Page);
                f1rfq_printing.CheckProgramPermissions(vPagerfq_printing, vUserIdrfq_printing, ref  vCanCreaterfq_printing, ref  vCanRunrfq_printing, ref  vCanUpdaterfq_printing, ref  vCanDeleterfq_printing, ref  PrmComprfq_printing, ref  aUsersrfq_printing);

                lblComp.Text = PrmComprfq_printing;
                //Response.Write(vCanRun);
                if (vCanRunrfq_printing == true)
                {
                    lnkrfq_printing.Visible = true;
                    //rfq_printing.Visible = true;
                }

                if (vCanRunrfq_printing == false)
                {
                    lnkrfq_printing.Visible = false;
                    //rfq_printing.Visible = false;

                }

                string vUserIdrfq_list_item = UserLogin.UserName;
                string vPagerfq_list_item = "rfq_list_item.aspx";
                string aUsersrfq_list_item = null;
                string PrmComprfq_list_item = null;
                bool vCanCreaterfq_list_item = false;
                bool vCanRunrfq_list_item = false;
                bool vCanUpdaterfq_list_item = false;
                bool vCanDeleterfq_list_item = false;

                func1 f1rfq_list_item = new func1();
                //Response.Write(Page);
                f1rfq_list_item.CheckProgramPermissions(vPagerfq_list_item, vUserIdrfq_list_item, ref  vCanCreaterfq_list_item, ref  vCanRunrfq_list_item, ref  vCanUpdaterfq_list_item, ref  vCanDeleterfq_list_item, ref  PrmComprfq_list_item, ref  aUsersrfq_list_item);

                lblComp.Text = PrmComprfq_list_item;
                //Response.Write(vCanRun);
                if (vCanRunrfq_list_item == true)
                {
                    lnk_listrfqitem.Visible = true;
                    //list_item.Visible = true;

                }

                if (vCanRunrfq_list_item == false)
                {
                    lnk_listrfqitem.Visible = false;
                    //list_item.Visible = false;

                }

            }
        }
    }

    protected void LinkButton1_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
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
        if (Request.Cookies["showmenu"] != null)
        {
            Response.Cookies["showmenu"].Expires = DateTime.Now.AddDays(-1);
        }
        Response.Redirect(sLoginURL);
    }
    //protected void lnkrfq_item_Click(object sender, EventArgs e)
    //{
    //    Response.Redirect("rftitem.aspx");
    //}
    protected void list_rfqs_Click(object sender, EventArgs e)
    {
        Response.Redirect("list_rfqs.aspx");
    }
    protected void lnkrfq_item_Click(object sender, EventArgs e)
    {
        Session["Rfqseq"] = Session["rfqsnos"];
        Response.Redirect("rfqitem.aspx");
    }
    protected void view_rfqs_Click(object sender, EventArgs e)
    {
        Session["Rfqview"] = Session["rfqsnos"];
        Response.Redirect("view_rfqs.aspx");
    }
    protected void rfq_size_Click(object sender, EventArgs e)
    {
        Session["Rfqsize"] = Session["rfqsnos"];
        Session["rfqcustpart"] = Session["list_rfq_grid_seqno"];
        Response.Redirect("rfq_size.aspx");
    }

    protected void rfq_itemspec_Click(object sender, EventArgs e)
    {
        Session["rfqspecno"] = Session["rfqsnos"];
        Session["rfqspecpart"] = Session["list_rfq_grid_seqno"];
        Response.Redirect("rfq_itemspec.aspx");
    }
    protected void lnkrfq_material_Click(object sender, EventArgs e)
    {
        Session["rfqmatno"] = Session["rfqsnos"];
        Session["rfqmatpart"] = Session["list_rfq_grid_seqno"];
        Response.Redirect("rfq_material.aspx");

    }
    protected void lnkrfq_printing_Click(object sender, EventArgs e)
    {
        Session["rfqPrintno"] = Session["rfqsnos"];
        Session["rfqprintpart"] = Session["list_rfq_grid_seqno"];
        Response.Redirect("rfq_printing.aspx");

    }
    protected void lnkrfq_shipping_Click(object sender, EventArgs e)
    {
        Session["rfqshipno"] = Session["rfqsnos"];
        Session["rfqshippart"] = Session["list_rfq_grid_seqno"];
        Response.Write(Session["rfqshipno"]);
        Response.Redirect("rfq_ship.aspx");

    }

    protected void lnk_listitem_click(object sender, EventArgs e)
    {
        Session["Rfqseq"] = Session["rfqsnos"];
        Response.Redirect("rfq_list_item.aspx");
    }

    protected void ImageQuote_Click(object sender, EventArgs e)
    {
        Session["list_rfq_rfq_nos"] = Session["rfqsnos"];
        Response.Redirect("rfq_BrowseQuote.aspx");
    }
    protected void img_btn_add_click(object sender, ImageClickEventArgs e)
    {
        Session["add_rfq_list_buton"] = "add";
        Response.Redirect("view_rfqs.aspx");

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